import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { Post, User, Comment, PostCategory, PostFilterInput, PostSortInput, PostConnection } from '../../schema'
import type { Context } from './types'
import { buildConnection, buildFilter, buildSort, buildSelect } from '@hakutakuai/zenstack-graphql/helpers'
import {
	POST_WITH_ALL_RELATIONS_SELECT,
	USER_WITH_ALL_RELATIONS_SELECT,
	COMMENT_WITH_RELATIONS_SELECT,
	POST_CATEGORY_WITH_RELATIONS_SELECT,
} from '../select-definitions'

@Resolver(() => Post)
export class PostResolver {
	@Query(() => PostConnection)
	async posts(
		@Arg('filter', () => PostFilterInput, { nullable: true }) filter: PostFilterInput | undefined,
		@Arg('sort', () => PostSortInput, { nullable: true }) sort: PostSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<PostConnection> {
		const where = buildFilter(filter as any)
		const orderBy = buildSort(sort as any)
		const select = buildSelect(POST_WITH_ALL_RELATIONS_SELECT, info)

		const config = buildConnection({
			first,
			after,
			last,
			before,
			where,
			orderBy,
			select,
		})

		const [items, totalCount] = await Promise.all([prisma.post.findMany(config.findMany), prisma.post.count(config.count)])

		return config.toConnection(items, totalCount) as PostConnection
	}

	@Query(() => Post, { nullable: true })
	async post(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return (await prisma.post.findUnique({ where: { id }, select: POST_WITH_ALL_RELATIONS_SELECT })) as Post | null
	}

	@Mutation(() => Post)
	async createPost(
		@Arg('title', () => String) title: string,
		@Arg('content', () => String) content: string,
		@Arg('authorId', () => String) authorId: string,
		@Arg('published', () => Boolean, { defaultValue: false }) published: boolean = false,
		@Ctx() { prisma }: Context,
	): Promise<Post> {
		return (await prisma.post.create({ data: { title, content, published, authorId }, select: POST_WITH_ALL_RELATIONS_SELECT })) as Post
	}

	@Mutation(() => Post, { nullable: true })
	async publishPost(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<Post | null> {
		return (await prisma.post.update({ where: { id }, data: { published: true }, select: POST_WITH_ALL_RELATIONS_SELECT })) as Post | null
	}

	@FieldResolver(() => User)
	async author(@Root() post: Post, @Ctx() { prisma }: Context): Promise<User | null> {
		return (await prisma.user.findUnique({ where: { id: post.authorId }, select: USER_WITH_ALL_RELATIONS_SELECT })) as User | null
	}

	@FieldResolver(() => [PostCategory])
	async categories(@Root() post: Post, @Ctx() { prisma }: Context): Promise<PostCategory[]> {
		return (await prisma.categoryOnPost.findMany({ where: { postId: post.id }, select: POST_CATEGORY_WITH_RELATIONS_SELECT })) as PostCategory[]
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() post: Post, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return (await prisma.comment.findMany({
			where: { postId: post.id },
			select: COMMENT_WITH_RELATIONS_SELECT,
		})) as Comment[]
	}
}
