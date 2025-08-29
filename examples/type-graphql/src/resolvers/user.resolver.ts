import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { User, Post, Comment, UserFilterInput, UserSortInput, UserConnection } from '../../schema'
import type { Context } from './types'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import { USER_WITH_ALL_RELATIONS_SELECT, POST_WITH_ALL_RELATIONS_SELECT, COMMENT_WITH_RELATIONS_SELECT } from '../select-definitions'

@Resolver(() => User)
export class UserResolver {
	@Query(() => UserConnection)
	async users(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | undefined,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(USER_WITH_ALL_RELATIONS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([prisma.user.findMany(config.findMany), prisma.user.count(config.count)])

		return config.toConnection(items, totalCount) as UserConnection
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		const user = await prisma.user.findUnique({
			where: { id },
			select: USER_WITH_ALL_RELATIONS_SELECT,
		})
		return user as User | null
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		const user = await prisma.user.create({
			data: { name, email, bio },
			select: USER_WITH_ALL_RELATIONS_SELECT,
		})
		return user as User
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		const posts = await prisma.post.findMany({
			where: { authorId: user.id },
			select: POST_WITH_ALL_RELATIONS_SELECT,
		})
		return posts as Post[]
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		const comments = await prisma.comment.findMany({
			where: { authorId: user.id },
			select: COMMENT_WITH_RELATIONS_SELECT,
		})
		return comments as Comment[]
	}
}
