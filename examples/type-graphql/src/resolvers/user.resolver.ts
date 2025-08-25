import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int } from 'type-graphql'
import { User, Post, Comment, UserFilterInput, UserSortInput, UserConnection } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { PRISMA_INCLUDES } from '../utils/resolver-helpers'

@Resolver(() => User)
export class UserResolver extends BaseResolver {
	private readonly ALLOWED_FILTER_FIELDS = ['email', 'name', 'createdAt']
	private readonly ALLOWED_SORT_FIELDS = ['createdAt', 'updatedAt']

	@Query(() => UserConnection)
	async users(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | null,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | null,
		@Arg('first', () => Int, { nullable: true }) first: number | null,
		@Arg('after', () => String, { nullable: true }) after: string | null,
		@Arg('last', () => Int, { nullable: true }) last: number | null,
		@Arg('before', () => String, { nullable: true }) before: string | null,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		return this.buildRelayConnection<User>(
			prisma,
			'user',
			{ filter, sort, first, after, last, before },
			this.ALLOWED_FILTER_FIELDS,
			this.ALLOWED_SORT_FIELDS,
		)
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id })
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		return this.create<User>(prisma, 'user', { name, email, bio })
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		return this.findMany<Post>(prisma, 'post', {
			where: { authorId: user.id },
			include: PRISMA_INCLUDES.POST,
		})
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return this.findMany<Comment>(prisma, 'comment', {
			where: { authorId: user.id },
			include: PRISMA_INCLUDES.COMMENT,
		})
	}
}
