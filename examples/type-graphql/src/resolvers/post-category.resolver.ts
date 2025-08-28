import { Resolver, Query, FieldResolver, Root, Ctx, Arg, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { PostCategory, Post, Category, PostCategoryConnection, PostCategoryQueryArgs } from '../../schema'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, CATEGORY_INCLUDES } from '../../schema-helpers'

@Resolver(() => PostCategory)
export class PostCategoryResolver {
	@Query(() => PostCategoryConnection)
	async postCategories(
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<PostCategoryConnection> {
		const args: PostCategoryQueryArgs = { first, after, last, before }
		const config = ConnectionBuilder.buildPostCategoryConnectionConfig(args, info)

		const items = await prisma.categoryOnPost.findMany(config.findManyOptions)
		const totalCount = await prisma.categoryOnPost.count(config.countOptions)

		return ConnectionBuilder.processResults(items, totalCount, config.paginationInfo) as PostCategoryConnection
	}

	@FieldResolver(() => Post)
	async post(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Post | null> {
		return await prisma.post.findUnique({ where: { id: postCategory.postId }, include: POST_INCLUDES }) as Post | null
	}

	@FieldResolver(() => Category)
	async category(@Root() postCategory: PostCategory, @Ctx() { prisma }: Context): Promise<Category | null> {
		return await prisma.category.findUnique({ where: { id: postCategory.categoryId }, include: CATEGORY_INCLUDES }) as Category | null
	}
}
